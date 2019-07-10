using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class invspec_list_notes : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
      
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "invspec_list_notes.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                lblUser.Text = UserLogin.UserName;
                lblComp.Text = PrmComp;
                if (aUsers == "external")
                {
                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }
            }
        }
        catch { }
        try
        {
            if (Session["invspec_list_notes_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["invspec_list_notes_date"] = GridView1.SelectedRow.Cells[1].Text;
                Session["invspec_list_notes_title"] = GridView1.SelectedRow.Cells[3].Text;
                Session["invspec_view_notes_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;

                DateTime time = DateTime.Parse(GridView1.SelectedRow.Cells[2].Text);
                Int32 hrtosec = Convert.ToInt32(time.Hour) * 3600;
                Int32 mintosec = Convert.ToInt32(time.Minute) * 60;
                Int32 sec = Convert.ToInt32(time.Second);
                Int32 totaltime = hrtosec + mintosec + sec;
                //Response.Write(totaltime);
                Session["invspec_list_notes_time"] = totaltime;
            }
            GridView1.SelectedIndex = Convert.ToInt32(Session["invspec_list_notes_index"]);
        }
        catch { }
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["invspec_list_notes_index"] = GridView1.SelectedIndex;
        Session["invspec_list_notes_date"] = GridView1.SelectedRow.Cells[1].Text;
        //Session["order_list_notes_time"] = GridView1.SelectedRow.Cells[2].Text;
        Session["invspec_list_notes_title"] = GridView1.SelectedRow.Cells[3].Text;
        Session["invspec_view_notes_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        
        DateTime time = DateTime.Parse(GridView1.SelectedRow.Cells[2].Text);
        Int32 hrtosec = Convert.ToInt32(time.Hour) * 3600;
        Int32 mintosec = Convert.ToInt32(time.Minute) * 60;
        Int32 sec = Convert.ToInt32(time.Second);
        Int32 totaltime = hrtosec + mintosec + sec;
        //Response.Write(totaltime);
        Session["invspec_list_notes_time"] = totaltime;
        
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSelect";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = txt_dept_spec.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = grp_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = txt_note_date.Text.Trim();
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        txt_dept_spec.Text = "";
        txt_note_date.Text = "";
        grp_TextBox.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "GridSelect";
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = txt_dept_spec.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = grp_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = txt_note_date.Text.Trim();
    }
    protected void img_btn_list_notes_click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("invspec_list_notes.aspx");
    }
    protected void img_btn_view_notes_click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("invspec_view_notes.aspx");
    }
    
}
