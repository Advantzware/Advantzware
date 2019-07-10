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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;
/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqestimate : System.Web.UI.Page
{

    public rfqestimate()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    protected void Page_PreRender(object sender, EventArgs e)
    {
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow gvrow = GridView1.Rows[i];
            bool ischecked = ((CheckBox)gvrow.FindControl("chkSelect")).Checked;
            if (ischecked)
            {
                Session["list_rfq_old_est"] = gvrow.Cells[10].Text;

                if (Convert.ToString(Session["list_rfq_old_est"]) != "" && Convert.ToString(Session["list_rfq_old_est"]) != "&nbsp;")
                {
                    Session["my_new_est_no"] = 1;
                }
                else
                {
                    Session["my_new_est_no"] = null;
                }
            }
        }

        if (Session["my_new_est_no"] == null)
        {
            Button1.Visible = false;
        }
        else
        {
            if (RadioButtonList2.SelectedIndex == 0)
            {
                Button1.Visible = true;
                Transfertoestimate.Visible = false;
            }
            if (RadioButtonList2.SelectedIndex == 1)
            {
                Button1.Visible = false;
                Transfertoestimate.Visible = true;
            }
        }
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        //Response.Write(Session["my_new_est_no"]);
        //Response.Write(Session["list_rfq_old_est"]);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfq_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);           

            Session["rfq_estimate_comp"] = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            try
            {             
                if (Convert.ToString(Session["list_rfq_cust_style"]) == "2")
                {
                    // DropDownList2.SelectedIndex = 0;

                    DropDownList2.Items.RemoveAt(0);
                    DropDownList2.Items.RemoveAt(1);
                    DropDownList2.Items.RemoveAt(0);
                    DropDownList2.Items.RemoveAt(0);
                }
                if (Convert.ToString(Session["list_rfq_cust_style"]) == "1")
                {
                    //DropDownList2.SelectedIndex = 0;
                    DropDownList2.Items.RemoveAt(4);
                    DropDownList2.Items.RemoveAt(4);
                }

                if (Session["my_new_est_no"] != null)
                {
                    Transfertoestimate.Visible = false;

                }
                else
                {
                    Button1.Visible = false;

                }

            }
            catch { }




        }
        if (!Page.IsPostBack)
        {
            //Session["list_rfq_old_est"] = null;

            RadioButtonList2.SelectedIndex = 0;
            try
            {
                if (Convert.ToInt32(Session["list_rfq_grid_row_count"]) == 1)
                {
                    for (int i = 0; i < 1; i++)
                    {
                        CheckBox ck = (CheckBox)GridView1.Rows[i].FindControl("chkSelect");
                        ck.Checked = true;

                    }
                }
            }
            catch { }
        }
    }

    protected void transfertoestimate_Click(object sender, EventArgs e)
    {
        StringBuilder s1 = new StringBuilder();
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow gvrow = GridView1.Rows[i];
            bool ischecked = ((CheckBox)gvrow.FindControl("chkSelect")).Checked;
            if (ischecked)
            {
                s1.Append(gvrow.Cells[4].Text);
                s1.Append(",");
                //Session["list_rfq_old_est"] = gvrow.Cells[10].Text;
            }
            else
            {
                CheckLabel.Text = "Please select atleast one checkbox";
            }

        }

        if (RadioButtonList2.SelectedIndex == 0)
        {
            HiddenField1.Value = "Yes";
        }
        if (RadioButtonList2.SelectedIndex == 1)
        {
            HiddenField1.Value = "No";
        }

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "TransferEstimate";
        ObjectDataSource1.SelectParameters["prmEstType"].DefaultValue = DropDownList2.SelectedValue;
        ObjectDataSource1.SelectParameters["prmEstNew"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmSeqList"].DefaultValue = Convert.ToString(s1);

        ////Response.Write("<script>window.location.href='rfq_estimate.aspx'</script>");
    }

}




