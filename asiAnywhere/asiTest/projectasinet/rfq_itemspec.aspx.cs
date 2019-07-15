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

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqitemspec : System.Web.UI.Page
{
    public rfqitemspec()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        
        Session["my_new_rfq"] = null;
        //ImageButton rfqitemspec = (ImageButton)Master.FindControl("rfq_itemspec");
        //rfqitemspec.ImageUrl = "~/Images/rfqitem spec1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq ItemSpec";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "rfq_itemspec.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                //lblComp.Text = PrmComp;
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
        }
    }

    protected void UpdateButton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {


            string vUserId = UserLogin.UserName;
            string vPage = "releases.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;




            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

        }
        Label Part = (Label)FormView1.FindControl("part_noTextBox");
        TextBox Name = (TextBox)FormView1.FindControl("i_nameTextBox");
        TextBox dscr = (TextBox)FormView1.FindControl("part_dscr1TextBox");
        TextBox dscr2 = (TextBox)FormView1.FindControl("part_dscr2TextBox");
        TextBox dscr3 = (TextBox)FormView1.FindControl("part_dscr3TextBox");
        Label estimate = (Label)FormView1.FindControl("est_noTextBox");
        TextBox stock = (TextBox)FormView1.FindControl("stock_noTextBox");
        TextBox plate = (TextBox)FormView1.FindControl("plate_noTextBox");
        TextBox die = (TextBox)FormView1.FindControl("die_noTextBox");
        TextBox sample = (TextBox)FormView1.FindControl("cad_noTextBox");
        TextBox upc = (TextBox)FormView1.FindControl("upc_noTextBox");
        TextBox spc = (TextBox)FormView1.FindControl("spc_noTextBox");
        Label cat = (Label)FormView1.FindControl("procatTextBox");
        Label catdscr = (Label)FormView1.FindControl("ProcatDscrTextBox");
       

        //FormView1.ChangeMode(FormViewMode.Edit);

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";

        ObjectDataSource1.SelectParameters["PrmPartNo"].DefaultValue = Part.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = Name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr"].DefaultValue = dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr2"].DefaultValue = dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr3"].DefaultValue = dscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStock"].DefaultValue = stock.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlate"].DefaultValue = plate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDie"].DefaultValue = die.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSample"].DefaultValue = sample.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUpc"].DefaultValue = upc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSpc"].DefaultValue = spc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCat"].DefaultValue = cat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCatdscr"].DefaultValue = catdscr.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    

}