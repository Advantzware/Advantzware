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

public partial class ProgramMaster_main : System.Web.UI.Page
{
    private string strItem = "";
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }
        
        if (!Page.IsPostBack)
        {
            GridView1.SelectedIndex = 0;
            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

                string vUserId = UserLogin.UserName;
                string vPage = "ProgramMaster.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;




                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    //lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }

            }
            //btnField.Visible = (func1.CheckProgramPermissions(Page));
            //func.GetMenu(ddlQuickJump, "User Maintenance");

         //   hlnkExport.Visible = (func.CheckUserPermissions("[dbo].[user_master]", "p") && func.CheckUserPermissions("[dbo].[user_master]", "s"));

          //  hlnkPrint.Visible = (func.CheckUserPermissions("[dbo].[user_master]", "p") && func.CheckUserPermissions("[dbo].[user_master]", "s"));
//hlnkPrintImg.Visible = hlnkPrint.Visible;

         //   btnAdd.Visible = func.CheckUserPermissions("[dbo].[user_master]", "a");

          //  tdInfo.Visible = tdPageCount.Visible = func.CheckUserPermissions("[dbo].[user_master]", "s");

//tdSearch.Visible = tdInfo.Visible;
          //  hlnkAdvSearch.Visible = tdSearch.Visible;

           // btnDelete.Attributes.Add("onclick", "return confirm('" + "Do you really want to delete these records?" + "');");

           
        } //  ! Page.IsPostBack
        //try
        //{
        //    //GridView1_SelectedIndexChanged(sender, e);
        //    //FormView1.ChangeMode(FormViewMode.ReadOnly);
            
        //}
        //catch { return; }
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        GridView1.SelectedIndex = 0;
        objProgramMasterList.SelectParameters["prmAction"].DefaultValue = "search";
        objProgramMasterList.SelectParameters["programName"].DefaultValue = vProgramName.Text.Trim();
        objProgramMasterList.SelectParameters["title"].DefaultValue = vTitle.Text.Trim();
        objProgramMasterList.SelectParameters["viewId"].DefaultValue = vViewId.Text.Trim();
        objProgramMasterList.SelectParameters["addId"].DefaultValue = vAddId.Text.Trim();
        objProgramMasterList.SelectParameters["updateId"].DefaultValue = vUpdateId.Text.Trim();
        objProgramMasterList.SelectParameters["deleteId"].DefaultValue = vDeleteId.Text.Trim();

        
        Session["programName"] = vProgramName.Text.Trim();
        Session["title"] = vTitle.Text.Trim();
        Session["viewId"] = vViewId.Text.Trim();
        Session["addId"] = vAddId.Text.Trim();
        Session["updateId"] = vUpdateId.Text.Trim();
        Session["deleteId"] = vDeleteId.Text.Trim();

        
    }



    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";

        vProgramName.Text = str.ToString();
        vTitle.Text = str.ToString();
        vViewId.Text = str.ToString();
        vAddId.Text = str.ToString();
        vUpdateId.Text = str.ToString();
        vDeleteId.Text = str.ToString();

        GridView1.SelectedIndex = 0;
        objProgramMasterList.SelectParameters["prmAction"].DefaultValue = "select";
        objProgramMasterList.SelectParameters["programName"].DefaultValue = vProgramName.Text.Trim();
        objProgramMasterList.SelectParameters["title"].DefaultValue = vTitle.Text.Trim();
        objProgramMasterList.SelectParameters["viewId"].DefaultValue = vViewId.Text.Trim();
        objProgramMasterList.SelectParameters["addId"].DefaultValue = vAddId.Text.Trim();
        objProgramMasterList.SelectParameters["updateId"].DefaultValue = vUpdateId.Text.Trim();
        objProgramMasterList.SelectParameters["deleteId"].DefaultValue = vDeleteId.Text.Trim();

        Session["programName"] = vProgramName.Text.Trim();
        Session["title"] = vTitle.Text.Trim();
        Session["viewId"] = vViewId.Text.Trim();
        Session["addId"] = vAddId.Text.Trim();
        Session["updateId"] = vUpdateId.Text.Trim();
        Session["deleteId"] = vDeleteId.Text.Trim();
        
        
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        
        strItem = this.GridView1.SelectedValue.ToString();
        this.FormView1.DataBind();
        
    }

    protected void Mode_Changed(object sender, EventArgs e)
    {
        strItem = this.GridView1.SelectedValue.ToString();
        this.FormView1.DataBind();
        FormView1.ChangeMode(FormViewMode.Edit);
    }
    protected void Mode1_Changed(object sender, EventArgs e)
    {
        strItem = this.GridView1.SelectedValue.ToString();
        this.FormView1.DataBind();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void Mode2_Changed(object sender, EventArgs e)
    {
        GridView1.SelectedIndex=0;
    }
    protected void objProgramMasterDetail_Selecting(object sender, ObjectDataSourceSelectingEventArgs e)
    {
        e.InputParameters["prmAction"] = "Select";
        e.InputParameters["prmItem"] = strItem;
    }
    protected void FormView1_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {
        strItem = this.GridView1.SelectedValue.ToString();
        this.GridView1.DataBind();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        
    }
    protected void FormView1_ItemDeleted(object sender, FormViewDeletedEventArgs e)
    {
        this.GridView1.DataBind();
    }
    protected void FormView1_ItemInserted(object sender, FormViewInsertedEventArgs e)
    {
        
        this.GridView1.DataBind();
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
}
