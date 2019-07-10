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

public partial class spec_list_notes : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        Label_fgitem.Text =Convert.ToString(Session["spec_list_note_fg_item"]);
        //Response.Write(Session["order_rec_key"]);
        //UserClass.CheckLogin(Page);
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "spec_list_notes.aspx";
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
            if (Session["spec_list_notes_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["spec_list_notes_date"] = GridView1.SelectedRow.Cells[1].Text;
                //Session["order_list_notes_time"] = GridView1.SelectedRow.Cells[2].Text;
                Session["spec_list_notes_title"] = GridView1.SelectedRow.Cells[3].Text;

                DateTime time = DateTime.Parse(GridView1.SelectedRow.Cells[2].Text);
                Int32 hrtosec = Convert.ToInt32(time.Hour) * 3600;
                Int32 mintosec = Convert.ToInt32(time.Minute) * 60;
                Int32 sec = Convert.ToInt32(time.Second);
                Int32 totaltime = hrtosec + mintosec + sec;
                //Response.Write(totaltime);
                Session["spec_list_notes_time"] = totaltime;
            }
            GridView1.SelectedIndex = Convert.ToInt32(Session["spec_list_notes_index"]);
        }
        catch { }
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["spec_list_notes_index"] = GridView1.SelectedIndex;
        Session["spec_list_notes_date"] = GridView1.SelectedRow.Cells[1].Text;
        //Session["order_list_notes_time"] = GridView1.SelectedRow.Cells[2].Text;
        Session["spec_list_notes_title"] = GridView1.SelectedRow.Cells[3].Text;

        
        DateTime time = DateTime.Parse(GridView1.SelectedRow.Cells[2].Text);
        Int32 hrtosec = Convert.ToInt32(time.Hour) * 3600;
        Int32 mintosec = Convert.ToInt32(time.Minute) * 60;
        Int32 sec = Convert.ToInt32(time.Second);
        Int32 totaltime = hrtosec + mintosec + sec;
        //Response.Write(totaltime);
        Session["spec_list_notes_time"] = totaltime;
        
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = txt_dept_spec.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = txt_note_date.Text.Trim();
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
    }
    protected void img_btn_list_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("spec_list_notes.aspx");
    }
    protected void img_btn_view_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("spec_view_notes.aspx");
    }
    
}
