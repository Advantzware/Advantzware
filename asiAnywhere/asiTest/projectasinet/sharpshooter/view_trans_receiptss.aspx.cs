using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for vieworder
/// </summary>

namespace projectasinet.sharpshooter
{
    public partial class view_trans_receiptss : System.Web.UI.Page
    {

        protected void Page_Load(object sender, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";

            if (Convert.ToString(Session["view_trans_receipt_add_new_l"]) == "Add")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                Session["view_trans_receipt_add_new_l"] = null;
            }

            if (!Page.IsPostBack)
            {
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_daily_receipt.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();
                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (vCanRun == false)
                    {
                        Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                        Response.Write("<script>window.location.href = 'login.aspx';</script>");

                    }
                    lblComp.Text = PrmComp;
                    lblUser.Text = UserLogin.UserName;


                }

            }

        }


        protected void LinkButton1_Click(object sender, EventArgs e)
        {
            Response.Redirect("menu.aspx");
        }
        protected void hlnkLogOut_Click(object sender, EventArgs e)
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFileSs"];
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

        protected void lnk_list_click(object sender, EventArgs e)
        {
            Response.Redirect("trans_receipt_listss.aspx");
        }

        protected void lnk_view_click(object sender, EventArgs e)
        {
            Response.Redirect("view_trans_receiptss.aspx");
        }
        protected void Insert_Button_Click(object sender, EventArgs e)
        {
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
            TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
            TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
            TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            Label vdate = (Label)FormView1.FindControl("vDateTextBox");
            Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
            Label reckey = (Label)FormView1.FindControl("vRecKeyLabel");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            //Session["trans_recept_list_seq"] = reckey.Text.Trim();

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Addnewrcpt";
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = vtime.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = job2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = cust.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc2"].DefaultValue = toloc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin2"].DefaultValue = tolocbbin.Text.Trim();

            ObjectDataSource1.SelectParameters["prmTagno2"].DefaultValue = totag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
            FormView1.ChangeMode(FormViewMode.ReadOnly);



        }
        protected void Update_Button_Click(object sender, EventArgs e)
        {
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
            TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
            TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
            TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
            Label vdate = (Label)FormView1.FindControl("vDateTextBox");
            Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
            Label reckey = (Label)FormView1.FindControl("vRecKeyLabel");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];



            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = vtime.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = job2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = cust.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLoc2"].DefaultValue = toloc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLocBin2"].DefaultValue = tolocbbin.Text.Trim();

            ObjectDataSource1.SelectParameters["prmTagno2"].DefaultValue = totag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
            FormView1.ChangeMode(FormViewMode.ReadOnly);

        }
        protected void DeleteButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["trans_recept_list_seq"]);

        }
        protected void FormView1_PreRender(object sender, EventArgs e)
        {
            try
            {
                Label sql = (Label)FormView1.FindControl("vRecLabel");
                Session["trans_recept_list_seq"] = sql.Text;

            }
            catch { }
        }

        protected void FormView1_DataBound(object sender, EventArgs e)
        {
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                tag.Focus();

            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                tag.Focus();


                Label vdate = (Label)FormView1.FindControl("vDateTextBox");
                Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
                vdate.Text = DateTime.Now.ToShortDateString();
                vtime.Text = DateTime.Now.ToString("HH:MM");


            }
        }


        protected void TagTextBox_Change(object sender, EventArgs e)
        {
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
            TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
            TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
            TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
            TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
            TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
            TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
            TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
            TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");

            try
            {
                browspo look = new browspo();
                DataSet ds = new DataSet();
                ds = look.SelecttrnstagLook("PoSearch", Convert.ToString(Session["User"]), "Tag#", "EQUAL", tag.Text.Trim(), "", "");
                item.Text = ds.Tables[0].Rows[0][3].ToString();
                itemname.Text = ds.Tables[0].Rows[0][4].ToString();
                job.Text = ds.Tables[0].Rows[0][5].ToString();
                job2.Text = ds.Tables[0].Rows[0][6].ToString();
                loc.Text = ds.Tables[0].Rows[0][7].ToString();
                locbin.Text = ds.Tables[0].Rows[0][8].ToString();
                cases.Text = ds.Tables[0].Rows[0][11].ToString();
                qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
                totag.Text = ds.Tables[0].Rows[0][17].ToString();
                partial.Text = ds.Tables[0].Rows[0][14].ToString();
                itemname.Focus();
            }
            catch
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid Tag# Number')</script>");

            }

        }
        protected void Cancel_button_click(object sender, EventArgs e)
        {
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        protected void img_btn_add_click(object sender, EventArgs e)
        {
            FormView1.ChangeMode(FormViewMode.Insert);
        }


    }

}